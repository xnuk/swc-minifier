use anyhow::anyhow;
use swc_common::{sync::Lrc, FileName, SourceMap};
use swc_ecma_ast::{EsVersion, Expr, Program};
use swc_ecma_codegen::{
	text_writer::JsWriter, Config as CodegenConfig, Emitter as CodegenEmitter,
};
use swc_ecma_minifier::{
	optimize as minifier_optimize,
	option::{
		CompressOptions, ExtraOptions, MangleOptions, ManglePropertiesOptions,
		MinifyOptions, PureGetterOption, TopLevelOptions,
	},
};
use swc_ecma_parser::{parse_file_as_module, EsConfig, Syntax};
use swc_ecma_visit::VisitMutWith;

use serde::{de::Deserialize, Deserializer};
use serde_derive::Deserialize;
use serde_json::{from_value, json};

use std::{collections::HashMap, io};

const SANE_ECMA: Syntax = Syntax::Es(EsConfig {
	jsx: false,
	fn_bind: false,
	decorators: false,
	decorators_before_export: false,
	export_default_from: false,
	import_assertions: true,
	allow_super_outside_method: false,
	allow_return_outside_function: false,
});

fn parse_expr(s: impl AsRef<str>) -> Result<Box<Expr>, String> {
	use swc_common::source_map::BytePos;
	use swc_ecma_parser::{lexer::Lexer, Parser, StringInput};

	Parser::new_from(Lexer::new(
		SANE_ECMA,
		EsVersion::latest(),
		StringInput::new(s.as_ref(), BytePos::DUMMY, BytePos::DUMMY),
		None,
	))
	.parse_expr()
	.map_err(|e| format!("{e:?}"))
}

#[repr(transparent)]
#[derive(Hash, PartialEq, Eq)]
struct ExprBox(Box<Expr>);

impl<'de> Deserialize<'de> for ExprBox {
	fn deserialize<D>(deser: D) -> Result<Self, D::Error>
	where
		D: Deserializer<'de>,
	{
		let s: String = Deserialize::deserialize(deser)?;
		match parse_expr(s) {
			Ok(s) => Ok(ExprBox(s)),
			Err(e) => Err(<D::Error as serde::de::Error>::custom(e)),
		}
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

#[derive(Default, Deserialize)]
struct MiniOpt {
	#[serde(
		alias = "global_def",
		alias = "globals_def",
		alias = "globals_defs",
		default
	)]
	global_defs: HashMap<ExprBox, ExprBox>,

	#[serde(alias = "pure_func", default)]
	pure_funcs: Vec<ExprBox>,

	#[serde(default)]
	target: TargetVersion,
}

fn minify_option(opt: &MiniOpt) -> Result<MinifyOptions, String> {
	Ok(MinifyOptions {
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
			global_defs: opt
				.global_defs
				.iter()
				.map(|(k, v)| (k.0.clone(), v.0.clone()))
				.collect(),
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
			pure_funcs: opt.pure_funcs.iter().map(|v| v.0.clone()).collect(),
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
			top_level: true,
			keep_class_names: false,
			keep_fn_names: false,
			keep_private_props: false,
			ie8: false,
			safari10: false,
			reserved: Vec::new(),
		}),

		wrap: false,
		enclose: false,
	})
}

const fn codegen_config(target: TargetVersion) -> CodegenConfig {
	CodegenConfig {
		target: target.0,
		ascii_only: false,
		minify: true,
		omit_last_semi: true,
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

fn minify(
	input: &mut impl io::Read,
	opt: MiniOpt,
	output: impl io::Write,
) -> anyhow::Result<()> {
	use swc_ecma_transforms_base::{fixer::fixer, resolver};

	let mut source = String::new();
	input.read_to_string(&mut source)?;
	let sourcemap: Lrc<SourceMap> = Default::default();

	let file = sourcemap.new_source_file(FileName::Anon, source);
	let mut errors = Vec::new();
	let res = parse_file_as_module(
		&file,
		SANE_ECMA,
		EsVersion::latest(),
		None,
		&mut errors,
	);

	for error in &errors {
		eprintln!("{error:?}");
	}

	let mut module = res.map_err(|e| anyhow!("{e:?}"))?;

	with_extra_options(move |extra_option| {
		module.visit_mut_with(&mut resolver(
			extra_option.unresolved_mark,
			extra_option.top_level_mark,
			false,
		));

		let mut module = minifier_optimize(
			Program::Module(module),
			Default::default(),
			None,
			None,
			&minify_option(&opt).map_err(|e| anyhow!("{e}"))?,
			&extra_option,
		)
		.expect_module();

		module.visit_mut_with(&mut fixer(None));

		let writter = JsWriter::new(sourcemap.clone(), "", output, None);

		let mut emitter = CodegenEmitter {
			cfg: codegen_config(opt.target),
			cm: sourcemap,
			comments: None,
			wr: writter,
		};

		emitter.emit_module(&module)?;

		Ok(())
	})
}

fn main() -> anyhow::Result<()> {
	minify(
		&mut io::stdin(),
		// TODO: nice way to receive thiis
		from_value(json!({
			"global_def": {
				"typeof TextDecoder": "\"function\""
			},
			"pure_func": []
		}))?,
		io::stdout(),
	)
}
