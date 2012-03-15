package net.ohloh.ohcount4j.detect;

import java.util.HashMap;
import java.util.Map;

import net.ohloh.ohcount4j.OhcountException;
import net.ohloh.ohcount4j.io.Source;
import net.ohloh.ohcount4j.scan.*;

public class ExtensionDetector {

	private static Map<String, Class<? extends Scanner>> fixedMap = null;
	private static Map<String, Resolver> ambiguousMap = null;

	// Returns a dictionary of file extensions for which there is no doubt
	// which Scanner we will use.
	private static Map<String, Class<? extends Scanner>> getFixedMap() {
		if (fixedMap == null) {
			fixedMap = new HashMap<String, Class<? extends Scanner>>();

			// fixedMap.put("a",      AdaScanner.class);
			fixedMap.put("as",     ActionScriptScanner.class);
			fixedMap.put("ads",    AdaScanner.class);
			fixedMap.put("adb",    AdaScanner.class);
			fixedMap.put("asm",    AssemblyScanner.class);
			fixedMap.put("boo",    BooScanner.class);
			fixedMap.put("c",      CScanner.class);
			fixedMap.put("cs",     CSharpScanner.class);
			fixedMap.put("css",    CSSScanner.class);
			fixedMap.put("e",      EiffelScanner.class);
			fixedMap.put("erl",    ErlangScanner.class);
			fixedMap.put("fs",     FSharpScanner.class);
			fixedMap.put("groovy", GroovyScanner.class);
			fixedMap.put("htm",    HTMLScanner.class);
			fixedMap.put("html",   HTMLScanner.class);
			fixedMap.put("java",   JavaScanner.class);
			fixedMap.put("js",     JavaScriptScanner.class);
			fixedMap.put("lisp",   LispScanner.class);
			fixedMap.put("lua",    LuaScanner.class);
			fixedMap.put("m",      ObjectiveCScanner.class); // TODO: disambiguate MatlabScanner
			fixedMap.put("pas",    PascalScanner.class);
			fixedMap.put("pl",     PrologScanner.class);
			fixedMap.put("r",      RebolScanner.class);
			fixedMap.put("rb",     RubyScanner.class);
			fixedMap.put("ru",     RubyScanner.class);
			fixedMap.put("sql",    SqlScanner.class);
			fixedMap.put("st",     SmalltalkScanner.class);
			fixedMap.put("sh",     ShellScanner.class);
			fixedMap.put("tcl",    TclScanner.class);
			fixedMap.put("vb",     VisualBasicScanner.class);
			fixedMap.put("xml",    XmlScanner.class);
		}
		return fixedMap;
	}

	// Returns a dictionary of file extensions which are associated with more
	// then one possible Scanner (for instance, *.h may be C, C++, or Objective-C).
	//
	// Instead of Scanners, this dictionary contains Resolvers, which
	// must inspect the file contents to determine a final Scanner.
	private static Map<String, Resolver> getAmbiguousMap() {
		if (ambiguousMap == null) {
			ambiguousMap = new HashMap<String, Resolver>();

			ambiguousMap.put("h", new ExtnHResolver());
		}
		return ambiguousMap;
	}

	public static Class<? extends Scanner> detect(String extension, Source blob) throws OhcountException {
		Class<? extends Scanner> klass = getFixedMap().get(extension);
		if (klass == null) {
			Resolver resolver = getAmbiguousMap().get(extension);
			if (resolver != null) {
				klass = resolver.resolve(blob);
			}
		}
		return klass;
	}

}
