package net.ohloh.ohcount4j;

import java.util.ArrayList;
import java.util.List;

import net.ohloh.ohcount4j.scan.*;

public enum Language {

	/* All languages must be defined here.
	 *
	 * Each language must declare two mandatory properties:
	 *
	 *  - The language's official display name (niceName)
	 *  - A Scanner subclass capable of parsing this language
	 */
	ACTIONSCRIPT("ActionScript", ActionScriptScanner.class),
	ADA("Ada", AdaScanner.class),
	ASSEMBLY("Assembly", AssemblyScanner.class),
	BINARY("Binary", null), // Place holder for binary files. This "Language" will not trigger a scan.
	BOO("Boo", BooScanner.class),
	C("C", CStyleScanner.class),
	CPP("C++", CStyleScanner.class),
	CSHARP("C#", CStyleScanner.class),
	CSS("CSS", CStyleScanner.class),
	EIFFEL("Eiffel", EiffelScanner.class),
	ERLANG("Erlang", ErlangScanner.class),
	FSHARP("F#", FSharpScanner.class),
	GROOVY("Groovy", CStyleScanner.class),
	HTML("HTML", HTMLScanner.class),
	JAVA("Java", CStyleScanner.class),
	JAVASCRIPT("JavaScript", CStyleScanner.class),
	LIMBO("Limbo", CStyleScanner.class),
	LISP("Lisp", LispScanner.class),
	LUA("Lua", LuaScanner.class),
	MAKE("Make", MakeScanner.class),
	MATLAB("Matlab", MatlabScanner.class),
	OBJECTIVE_C("Objective-C", CStyleScanner.class),
	OCTAVE("Octave", MatlabScanner.class), // TODO. Octave also supports # comments
	PASCAL("Pascal", PascalScanner.class),
	PERL("Perl", GenericCodeScanner.class), // TODO.
	PHP("PHP", GenericCodeScanner.class), // TODO.
	PROLOG("Prolog", PrologScanner.class),
	PYTHON("Python", PythonScanner.class),
	REBOL("REBOL", RebolScanner.class),
	RUBY("Ruby", RubyScanner.class),
	SHELL("Shell", ShellScanner.class),
	SMALLTALK("Smalltalk", SmalltalkScanner.class),
	SQL("SQL", SqlScanner.class),
	TCL("Tcl", TclScanner.class),
	VB("VisualBasic", VisualBasicScanner.class),
	XML("XML", XmlScanner.class);

	/* Optional properties of languages are declared here.
	 *
	 * At a minimum, a language should define one or more file
	 * extensions or filenames associated with the language.
	 *
	 * You may also declare additional names (beyond the uname
	 * and niceName) by which the language might be known.
	 * These aliases can be matched against things like Emacs
	 * mode headers or shebang directives.
	 */
	static {
		ACTIONSCRIPT.extension("as");
		ADA.extensions("ada", "adb");
		ASSEMBLY.extension("asm");
		BINARY.extension("inc");
		BOO.extension("boo");
		C.extensions("c", "h");
		CPP.extensions("C", "c++", "cc", "cpp", "cxx", "H", "h", "h++", "hh", "hpp", "hxx");
		CSHARP.aliases("C#", "cs").extension("cs");
		CSS.extension("css");
		EIFFEL.extension("e");
		ERLANG.extension("erl");
		FSHARP.extension("fs");
		GROOVY.extension("groovy");
		HTML.extensions("htm", "html");
		JAVA.extension("java");
		JAVASCRIPT.alias("js").extension("js");
		LIMBO.extensions("b", "m");
		LUA.extension("lua");
		MAKE.filename("Makefile");
		OBJECTIVE_C.extensions("m", "h");
		OCTAVE.extensions("m", "octave");
		PASCAL.extension("pas");
		PERL.extension("pl"); // TODO. Obviously there are more; this triggers ExtnPLResolver.
		PHP.extension("inc"); // TODO. Obviously there are more; this triggers ExtIncResolver.
		PROLOG.extension("pl");
		PYTHON.extension("py");
		REBOL.extension("r");
		RUBY.alias("jruby").extensions("rb", "ru").filenames("Rakefile", "Gemfile");
		SQL.extension("sql");
		SMALLTALK.extension("st");
		SHELL.extensions("bash", "sh");
		TCL.extension("tcl");
		VB.extension("vb");
		XML.extension("xml");
	}

	private final String niceName;
	private final Class<? extends Scanner> scannerClass;
	private List<String> extensions;
	private List<String> filenames;
	private List<String> aliases;

	Language(String niceName, Class<? extends Scanner> scannerClass) {
		this.niceName = niceName;
		this.scannerClass = scannerClass;
		this.extensions = new ArrayList<String>();
		this.filenames = new ArrayList<String>();
		this.aliases = new ArrayList<String>();
	}

	public String uname() {
		return this.toString().toLowerCase();
	}

	public String niceName() {
		return niceName;
	}

	public Class<? extends Scanner> scannerClass() {
		return scannerClass;
	}

	public Scanner makeScanner() {
		try {
			Scanner scanner = this.scannerClass.newInstance();
			scanner.setDefaultLanguage(this);
			return scanner;
		} catch (InstantiationException e) {
			throw new OhcountException(e);
		} catch (IllegalAccessException e) {
			throw new OhcountException(e);
		}
	}

	public Language extension(String ext) {
		extensions.add(ext);
		return this;
	}

	public Language extensions(String... exts) {
		for (String ext : exts) {
			extension(ext);
		}
		return this;
	}

	public List<String> getExtensions() {
		return extensions;
	}

	public Language filename(String filename) {
		filenames.add(filename);
		return this;
	}

	public Language filenames(String... filenames) {
		for (String filename : filenames) {
			filename(filename);
		}
		return this;
	}

	public List<String> getFilenames() {
		return filenames;
	}

	public Language alias(String alias) {
		aliases.add(alias);
		return this;
	}

	public Language aliases(String... aliases) {
		for (String alias : aliases) {
			alias(alias);
		}
		return this;
	}

	public List<String> getAliases() {
		return aliases;
	}
}
