package net.ohloh.ohcount4j;

import java.util.HashMap;
import java.util.Map;

import net.ohloh.ohcount4j.scan.*;

public enum Language {
	ACTIONSCRIPT("ActionScript", ActionScriptScanner.class),
	ADA("Ada", AdaScanner.class),
	ASSEMBLY("Assembly", AssemblyScanner.class),
	BOO("Boo", BooScanner.class),
	C("C", CStyleScanner.class),
	CSHARP("C#", CStyleScanner.class),
	CSS("CSS", CStyleScanner.class),
	EIFFEL("Eiffel", EiffelScanner.class),
	ERLANG("Erlang", ErlangScanner.class),
	FSHARP("F#", FSharpScanner.class),
	GROOVY("Groovy", CStyleScanner.class),
	HTML("HTML", HTMLScanner.class),
	JAVA("Java", CStyleScanner.class),
	JAVASCRIPT("JavaScript", CStyleScanner.class),
	LISP("Lisp", LispScanner.class),
	LUA("Lua", LuaScanner.class),
	MAKE("Make", MakeScanner.class),
	MATLAB("Matlab", MatlabScanner.class),
	OBJECTIVE_C("Objective-C", CStyleScanner.class),
	PASCAL("Pascal", PascalScanner.class),
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

	private final String niceName;
	private final Class<? extends Scanner> scannerClass;

	private static Map<String, Language> extensionMap;
	private static Map<String, Language> filenameMap;
	private static Map<String, Language> nameMap;

	static {
		extensionMap = new HashMap<String, Language>();
		filenameMap = new HashMap<String, Language>();
		nameMap = new HashMap<String, Language>();

		for (Language language : Language.values()) {
			nameMap.put(language.uname().toLowerCase(), language);
			nameMap.put(language.niceName().toLowerCase(), language);
		}

		ACTIONSCRIPT
			.extension("as")
			;

		ADA
			.extension("ada")
			.extension("adb")
			;

		ASSEMBLY
			.extension("asm")
			;

		BOO
			.extension("boo")
			;

		C
			.extension("c")
			;

		CSHARP
			.alias("C#")
			.extension("cs")
			;

		CSS
			.extension("css")
			;

		ERLANG
			.extension("erl")
			;

		FSHARP
			.extension("fs")
			;

		GROOVY
			.extension("groovy")
			;

		HTML
			.extension("html")
			.extension("htm")
			;

		JAVA
			.extension("java")
			;

		JAVASCRIPT
			.alias("js")
			.extension("js")
			;

		LUA
			.extension("lua")
			;

		MAKE
			.filename("Makefile")
			;

		OBJECTIVE_C
			.extension("m")
			;

		PASCAL
			.extension("pas")
			;

		PROLOG
			.extension("pl")
			;

		PYTHON
			.extension("py")
			;

		REBOL
			.extension("r")
			;

		RUBY
			.alias("jruby")
			.extension("rb")
			.extension("ru")
			.filename("Rakefile")
			.filename("Gemfile")
			;

		SQL
			.extension("sql")
			;

		SMALLTALK
			.extension("st")
			;

		SHELL
			.extension("bash")
			.extension("sh")
			;

		TCL
			.extension("tcl")
			;

		VB
			.extension("vb")
			;

		XML
			.extension("xml")
			;
	}

	Language(String niceName, Class<? extends Scanner> scannerClass) {
		this.niceName = niceName;
		this.scannerClass = scannerClass;
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

	public Scanner makeScanner() throws OhcountException {
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
		extensionMap.put(ext, this);
		return this;
	}

	public static Language fromExtension(String ext) {
		return extensionMap.get(ext);
	}

	public Language filename(String filename) {
		filenameMap.put(filename, this);
		return this;
	}

	public static Language fromFilename(String filename) {
		return filenameMap.get(filename);
	}

	public Language alias(String alias) {
		nameMap.put(alias, this);
		return this;
	}

	public static Language fromName(String name) {
		if (name != null) {
			return nameMap.get(name.toLowerCase());
		} else {
			return null;
		}
	}
}
