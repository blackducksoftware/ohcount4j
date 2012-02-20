package net.ohloh.ohcount4j;

public enum Language {
	LANG_C("c", "C"),
	LANG_CPP("cpp", "C++"),
	LANG_CSS("css", "CSS"),
	LANG_HTML("html", "HTML"),
	LANG_JAVA("java", "Java"),
	LANG_MAKEFILE("make", "Make"),
	LANG_RUBY("ruby", "Ruby");

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
