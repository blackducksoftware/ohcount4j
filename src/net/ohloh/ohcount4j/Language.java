package net.ohloh.ohcount4j;

public enum Language {
	LANG_CPP("cpp", "C++"), LANG_JAVA("java", "Java"), LANG_C("c", "C"), LANG_MAKEFILE("make", "Make"), LANG_HTML(
			"html", "HTML");

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
