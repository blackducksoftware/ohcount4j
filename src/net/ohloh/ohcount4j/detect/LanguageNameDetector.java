package net.ohloh.ohcount4j.detect;

import java.util.HashMap;
import java.util.Map;

import net.ohloh.ohcount4j.scan.*;

// A fuzzy matcher. Given any ordinary descriptive name for a language (as might
// be found in a shebang line or Emacs mode header), find the best-guess Scanner.
public class LanguageNameDetector {

	private static Map<String, Class<? extends Scanner>> languageNameMap = null;

	private static Map<String, Class<? extends Scanner>> getLanguageNameMap() {
		if (languageNameMap == null) {
			languageNameMap = new HashMap<String, Class<? extends Scanner>>();

			languageNameMap.put("bash", ShellScanner.class);
			languageNameMap.put("c", CScanner.class);
			languageNameMap.put("cs", CSharpScanner.class);
			languageNameMap.put("css", CSSScanner.class);
			languageNameMap.put("csharp", CSharpScanner.class);
			languageNameMap.put("html", HTMLScanner.class);
			languageNameMap.put("javascript", JavaScriptScanner.class);
			languageNameMap.put("make", MakefileScanner.class);
			languageNameMap.put("ruby", RubyScanner.class);
			languageNameMap.put("shell", ShellScanner.class);
			languageNameMap.put("xml", XmlScanner.class);
		}
		return languageNameMap;
	}

	public static Class<? extends Scanner> detect(String filename) {
		if (filename == null) {
			return null;
		}
		return getLanguageNameMap().get(filename.toLowerCase());
	}
}