package net.ohloh.ohcount4j;

public enum Language {
	LANG_C("c", "C"),
	LANG_CPP("cpp", "C++"),
	LANG_CSS("css", "CSS"),
	LANG_HTML("html", "HTML"),
	LANG_JAVA("java", "Java"),
	LANG_JAVASCRIPT("javascript", "JavaScript"),
	LANG_MAKEFILE("make", "Make"),
	LANG_RUBY("ruby", "Ruby"),
	LANG_XML("xml", "XML"),
	LANG_CSHARP("cs", "C#"),
	LANG_SQL("sql", "SQL"),
	LANG_SHELL("shellscript", "ShellScript"),
<<<<<<< HEAD
	LANG_VB("vb", "VisualBasic");
	LANG_SMALLTALK("smalltalk", "Smalltalk");
	LANG_REBOL("rebol", "REBOL");
=======
	LANG_TCL("tcl", "Tcl");
>>>>>>> 6f009f42cd6fbc94539c7a41080d264b61919a4e

	private final String uname;
	private final String niceName;

	Language(String uname, String niceName) {
		this.uname = uname;
		this.niceName = niceName;
	}

	public String uname() {
		return uname;
	}

	public String niceName() {
		return niceName;
	}
}
