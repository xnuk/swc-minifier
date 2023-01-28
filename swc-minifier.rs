use swc_common::{
	collections::AHashMap, sync::Lrc, FileName, SourceMap, SourceMapper, Span,
};
use swc_ecma_ast::{EsVersion, Expr};
use swc_ecma_minifier::option::{ExtraOptions, MinifyOptions};
use swc_ecma_parser as ecma;

use serde::{de::Deserialize, Deserializer};
use serde_derive::Deserialize;

use std::{error, ffi::OsString, fmt, fs, io, marker::PhantomData, path::Path};

const SANE_ECMA: ecma::Syntax = ecma::Syntax::Es(ecma::EsConfig {
	jsx: false,
	fn_bind: false,
	decorators: false,
	decorators_before_export: false,
	export_default_from: false,
	import_assertions: true,
	allow_super_outside_method: false,
	allow_return_outside_function: false,
});

trait SpanToString {
	fn span_to_string(&self, span: Span) -> String;
}

impl<T: SourceMapper> SpanToString for T {
	fn span_to_string(&self, span: Span) -> String {
		SourceMapper::span_to_string(self, span)
	}
}

struct StringSource<'a>(FileName, &'a str);

impl<'a> SpanToString for StringSource<'a> {
	fn span_to_string(&self, span: Span) -> String {
		let sourcemap = SourceMap::default();
		sourcemap.new_source_file(self.0.clone(), self.1.to_string());
		SourceMap::span_to_string(&sourcemap, span)
	}
}

fn error_to_string(
	sourcemap: &impl SpanToString,
	err: &swc_ecma_parser::error::Error,
) -> String {
	use swc_common::Spanned;

	let mut res = sourcemap.span_to_string(err.span());
	res.push_str(": ");
	res.push_str(&err.kind().msg());

	res
}

fn parse_expr(s: impl AsRef<str>) -> Result<Box<Expr>, String> {
	use swc_common::source_map::BytePos;
	use swc_ecma_parser::{lexer::Lexer, Parser, StringInput};

	let source = s.as_ref();

	Parser::new_from(Lexer::new(
		SANE_ECMA,
		EsVersion::latest(),
		StringInput::new(source, BytePos::DUMMY, BytePos::DUMMY),
		None,
	))
	.parse_expr()
	.map_err(|e| {
		error_to_string(
			&StringSource(FileName::Internal("expr".into()), source),
			&e,
		)
	})
}

#[repr(transparent)]
#[derive(Hash, PartialEq, Eq)]
struct ExprBox(Box<Expr>);

impl TryFrom<String> for ExprBox {
	type Error = String;
	fn try_from(value: String) -> Result<Self, Self::Error> {
		parse_expr(value).map(ExprBox)
	}
}

impl<'de> Deserialize<'de> for ExprBox {
	fn deserialize<D>(deser: D) -> Result<Self, D::Error>
	where
		D: Deserializer<'de>,
	{
		Deser::<String, _>::deser_try(deser)
	}
}

impl From<ExprBox> for Box<Expr> {
	fn from(value: ExprBox) -> Self {
		value.0
	}
}

#[repr(transparent)]
#[derive(Clone, Copy, Deserialize)]
struct TargetVersion(EsVersion);

impl Default for TargetVersion {
	fn default() -> Self {
		TargetVersion(EsVersion::latest())
	}
}

impl From<TargetVersion> for EsVersion {
	fn from(value: TargetVersion) -> Self {
		value.0
	}
}

#[derive(Clone, Deserialize)]
#[serde(untagged)]
pub enum OneOrMany<T> {
	One(T),
	Many(Vec<T>),
}

impl<T> From<OneOrMany<T>> for Vec<T> {
	fn from(value: OneOrMany<T>) -> Self {
		match value {
			OneOrMany::One(x) => Vec::from([x]),
			OneOrMany::Many(vec) => vec,
		}
	}
}

impl<T> IntoIterator for OneOrMany<T> {
	type Item = T;
	type IntoIter = <Vec<T> as IntoIterator>::IntoIter;

	fn into_iter(self) -> Self::IntoIter {
		let vec = Vec::from(self);
		vec.into_iter()
	}
}

impl<T> Default for OneOrMany<T> {
	fn default() -> Self {
		OneOrMany::Many(Vec::default())
	}
}

struct Deser<I, O> {
	input: PhantomData<I>,
	output: PhantomData<O>,
}

impl<'de, O: From<I>, I: Deserialize<'de>> Deser<I, O> {
	fn deser<D: Deserializer<'de>>(deser: D) -> Result<O, D::Error> {
		let input: I = Deserialize::deserialize(deser)?;
		Ok(O::from(input))
	}
}

impl<
		'de,
		O: From<I>,
		I,
		WI: IntoIterator<Item = I> + Deserialize<'de>,
		WO: IntoIterator<Item = O> + FromIterator<O>,
	> Deser<WI, WO>
{
	fn deser_seq<D: Deserializer<'de>>(deser: D) -> Result<WO, D::Error> {
		let input: WI = Deserialize::deserialize(deser)?;
		Ok(WO::from_iter(input.into_iter().map(|i: I| O::from(i))))
	}
}

impl<
		'de,
		O: From<I>,
		I,
		WI: IntoIterator<Item = (I, I)> + Deserialize<'de>,
		WO: IntoIterator<Item = (O, O)> + FromIterator<(O, O)>,
	> Deser<WI, WO>
{
	fn deser_map<D: Deserializer<'de>>(deser: D) -> Result<WO, D::Error> {
		let input: WI = Deserialize::deserialize(deser)?;
		Ok(WO::from_iter(
			input
				.into_iter()
				.map(|(i, j): (I, I)| (O::from(i), O::from(j))),
		))
	}
}

impl<'de, E: ToString, O: TryFrom<I, Error = E>, I: Deserialize<'de>>
	Deser<I, O>
{
	fn deser_try<D: Deserializer<'de>>(deser: D) -> Result<O, D::Error> {
		let input: I = Deserialize::deserialize(deser)?;
		O::try_from(input)
			.map_err(|e| <D::Error as serde::de::Error>::custom(e.to_string()))
	}
}

#[derive(Default, Deserialize)]
struct MiniOpt {
	#[serde(
		alias = "global_def",
		alias = "globals_def",
		alias = "globals_defs",
		deserialize_with = "Deser::<AHashMap<ExprBox, ExprBox>, _>::deser_map",
		default
	)]
	global_defs: AHashMap<Box<Expr>, Box<Expr>>,

	#[serde(
		alias = "pure_func",
		deserialize_with = "Deser::<OneOrMany<ExprBox>, _>::deser_seq",
		default
	)]
	pure_funcs: Vec<Box<Expr>>,

	#[serde(
		alias = "targets",
		deserialize_with = "Deser::<TargetVersion, _>::deser",
		default = "EsVersion::latest"
	)]
	target: EsVersion,
}

fn minify_option(opt: MiniOpt) -> MinifyOptions {
	use swc_ecma_minifier::option::{
		CompressOptions, MangleOptions, ManglePropertiesOptions,
		PureGetterOption, TopLevelOptions,
	};

	MinifyOptions {
		rename: true,
		compress: Some(CompressOptions {
			arguments: false,
			arrows: true,
			bools: true,
			bools_as_ints: false,
			collapse_vars: true,
			comparisons: true,
			computed_props: true,
			conditionals: true,
			dead_code: true,
			directives: true,
			drop_console: false,
			drop_debugger: true,
			ecma: EsVersion::latest(),
			evaluate: true,
			expr: true,
			global_defs: opt.global_defs,
			hoist_fns: false,
			hoist_props: true,
			hoist_vars: false,
			ie8: false,
			if_return: true,
			inline: 1,
			join_vars: true,
			keep_classnames: false,
			keep_fargs: false,
			keep_fnames: false,
			keep_infinity: false,
			loops: true,
			module: true,
			negate_iife: true,
			passes: 0,
			props: true,
			pure_getters: PureGetterOption::Strict,
			pure_funcs: opt.pure_funcs,
			reduce_fns: true,
			reduce_vars: true,
			sequences: 200,
			side_effects: true,
			switches: true,
			top_retain: Vec::new(),
			top_level: Some(TopLevelOptions { functions: true }),
			typeofs: true,
			unsafe_passes: true,
			unsafe_arrows: true,
			unsafe_comps: true,
			unsafe_function: true,
			unsafe_math: true,
			unsafe_symbols: true,
			unsafe_methods: true,
			unsafe_proto: true,
			unsafe_regexp: true,
			unsafe_undefined: true,
			unused: true,
			const_to_let: true,
			pristine_globals: true,
		}),
		mangle: Some(MangleOptions {
			props: Some(ManglePropertiesOptions {
				reserved: Vec::new(),
				undeclared: None,
				regex: None,
			}),
			top_level: Some(true),
			keep_class_names: false,
			keep_fn_names: false,
			keep_private_props: false,
			ie8: false,
			safari10: false,
			reserved: Vec::new(),
		}),

		wrap: false,
		enclose: false,
	}
}

fn with_extra_options<T>(func: impl FnOnce(ExtraOptions) -> T) -> T {
	use swc_common::{hygiene::Mark, Globals, GLOBALS};
	GLOBALS.set(&Globals::default(), || {
		func(ExtraOptions {
			top_level_mark: Mark::new(),
			unresolved_mark: Mark::new(),
		})
	})
}

#[repr(transparent)]
struct Output(Vec<u8>);

struct Messages {
	warns: Vec<String>,
	result: Result<Output, String>,
}

fn minify(source: String, opt: MiniOpt) -> Messages {
	// use swc_common::comments::{Comments, SingleThreadedComments};
	use swc_ecma_ast::Program;
	use swc_ecma_codegen::{
		self as codegen,
		text_writer::{
			// why the fuck
			omit_trailing_semi,
			JsWriter,
		},
	};
	use swc_ecma_parser::{Parser, StringInput};

	let target = opt.target;

	let sourcemap = Lrc::<SourceMap>::default();

	// TODO: leading userscript comments, pure comments
	// let comments = SingleThreadedComments::default();
	// let dyn_comments = Some(&comments as &dyn Comments);

	let dyn_comments = None;

	let file =
		sourcemap.new_source_file(FileName::Real("source".into()), source);

	let mut parser =
		Parser::new(SANE_ECMA, StringInput::from(&*file), dyn_comments);
	let module = parser.parse_module();

	let warns: Vec<String> = {
		let sourcemap = sourcemap.as_ref();
		parser
			.take_errors()
			.drain(0..)
			.map(|err| error_to_string(sourcemap, &err))
			.collect()
	};

	let mut module = match module {
		Ok(res) => res,
		Err(error) => {
			let error = error_to_string(sourcemap.as_ref(), &error);
			return Messages {
				warns,
				result: Err(error),
			};
		}
	};

	let module = with_extra_options(|extra_option| {
		use swc_ecma_minifier::optimize;
		use swc_ecma_transforms_base::{fixer::fixer, resolver};
		use swc_ecma_visit::VisitMutWith;

		module.visit_mut_with(&mut resolver(
			extra_option.unresolved_mark,
			extra_option.top_level_mark,
			false,
		));

		let mut module = optimize(
			Program::Module(module),
			Default::default(),
			dyn_comments,
			None,
			&minify_option(opt),
			&extra_option,
		)
		.expect_module();

		module.visit_mut_with(&mut fixer(None));
		module
	});

	let mut out = Vec::new();

	let emitted = codegen::Emitter {
		cfg: codegen::Config {
			target,
			ascii_only: false,
			minify: true,
			omit_last_semi: true,
		},
		comments: dyn_comments,
		wr: omit_trailing_semi(JsWriter::new(
			sourcemap.clone(),
			"\n",
			&mut out,
			None,
		)),
		cm: sourcemap,
	}
	.emit_module(&module);

	Messages {
		warns,
		result: emitted.map(|_| Output(out)).map_err(|v| v.to_string()),
	}
}

fn file_reader(path: impl AsRef<Path>) -> io::Result<io::BufReader<fs::File>> {
	Ok(io::BufReader::new(fs::File::open(path)?))
}

fn argf(path: Option<impl AsRef<Path>>) -> io::Result<Vec<u8>> {
	use std::io::Read;

	let mut source = Vec::new();

	match path {
		Some(path) => file_reader(path)?.read_to_end(&mut source),
		None => io::stdin().lock().read_to_end(&mut source),
	}?;

	Ok(source)
}

#[repr(transparent)]
#[derive(Debug)]
struct AlreadyHasBeen(String);

impl fmt::Display for AlreadyHasBeen {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "The option `{}` can be set only once.", self.0)
	}
}

impl error::Error for AlreadyHasBeen {}

impl From<AlreadyHasBeen> for lexopt::Error {
	#[inline]
	fn from(value: AlreadyHasBeen) -> Self {
		lexopt::Error::Custom(Box::new(value))
	}
}

fn expect_empty<T>(val: &Option<T>, name: &str) -> Result<(), lexopt::Error> {
	if val.is_none() {
		Ok(())
	} else {
		Err(AlreadyHasBeen(name.into()).into())
	}
}

const BIN_NAME: &str = env!("CARGO_BIN_NAME");
const BIN_VERSION: &str = env!("CARGO_PKG_VERSION");

const VERSION_INFO: &[u8] =
	const_str::concat_bytes!(BIN_NAME, " v", BIN_VERSION);

const HELP_MESSAGE: &[u8] = const_str::concat_bytes!(
	"Usage:\n\t",
	BIN_NAME,
	r#" [options] [--] [file]

OPTIONS
	[file]       input file. uses STDIN if not given.

	--help       you are reading this
	--version    print version

	--config=CONFIG, --option=CONFIG
		JSON minify options, file path or inline.
		anything starting with { considered as inline JSON.

CONFIG
	Must be valid JSON key-value object.
	Recommend using other tools to convert to JSON.
	Every config keys can be end with trailing `s` in each words.
		(ex. "globals_def", "global_defs", "globals_defs"
		are all the same thing, but should appear only one in the config.)

	"global_def" object (default: {})
		Key will be replaced to value.
		Every key or value in this object will be treated as JS expressions,
		so unlike Terser, `@` prefix will cause parsing error.

		Wrap with quotes for string.

		Example:
			{ "global_def": { "typeof TextEncoder": "\"function\"" } }

	"pure_func" array (default: [])
		List of functions which can be removed if not used.

	"target" string (default: "es2022")
		Output ECMAScript version. Can be: es3, es5, es2015, es2016, ...
"#,
	b'\n'
);

enum SungsimdangArg {
	Help,
	Version,
	Data((Option<OsString>, Option<OsString>)),
}

fn parse_args() -> Result<SungsimdangArg, lexopt::Error> {
	use lexopt::{Arg, Parser};

	let mut option = None;
	let mut input_path = None;
	let mut parser = Parser::from_env();
	while let Some(arg) = parser.next()? {
		match arg {
			Arg::Long("help") => return Ok(SungsimdangArg::Help),
			Arg::Long("version") => return Ok(SungsimdangArg::Version),
			Arg::Long("option") | Arg::Long("config") => {
				expect_empty(&option, "option")?;
				option = Some(parser.value()?);
			}

			Arg::Value(val) => {
				expect_empty(&input_path, "FILE")?;
				input_path = Some(val);
			}

			_ => return Err(arg.unexpected()),
		}
	}

	Ok(SungsimdangArg::Data((option, input_path)))
}

fn resolve_miniopt(arg: Option<OsString>) -> anyhow::Result<MiniOpt> {
	if let Some(arg) = arg {
		let inline = arg.to_str().and_then(|v| {
			if v.trim_start().starts_with('{') {
				Some(v)
			} else {
				None
			}
		});

		if let Some(inline) = inline {
			serde_json::from_str(inline)
		} else {
			serde_json::from_reader(&mut file_reader(arg)?)
		}
		.map_err(|v| v.into())
	} else {
		Ok(Default::default())
	}
}

fn main() -> anyhow::Result<()> {
	use std::io::Write;
	let (opt, source) = {
		match parse_args()? {
			SungsimdangArg::Help => {
				io::stdout().write_all(HELP_MESSAGE)?;
				return Ok(());
			}
			SungsimdangArg::Version => {
				io::stdout().write_all(VERSION_INFO)?;
				return Ok(());
			}
			SungsimdangArg::Data((option, input_path)) => (
				resolve_miniopt(option)?,
				String::from_utf8(argf(input_path)?)?,
			),
		}
	};

	if source.trim().is_empty() {
		io::stderr().write_all(HELP_MESSAGE)?;
		return Err(anyhow::Error::msg("input is somehow empty."));
	}

	let mut res = minify(source, opt);

	for mut warn in res.warns.drain(0..) {
		warn.push('\n');
		io::stderr().write_all(warn.as_bytes())?;
	}

	match res.result {
		Err(err) => Err(anyhow::Error::msg(err)),
		Ok(source) => {
			io::stdout().write_all(&source.0)?;
			Ok(())
		}
	}
}
