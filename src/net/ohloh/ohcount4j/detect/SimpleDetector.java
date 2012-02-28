package net.ohloh.ohcount4j.detect;

import net.ohloh.ohcount4j.io.Blob;
import net.ohloh.ohcount4j.scan.*;

public class SimpleDetector {

	public static Scanner detect(Blob blob) {

		String extension = blob.getExtension();

		if (extension.equals("c")) {
			return new CScanner();
		} else if (extension.equals("css")) {
			return new CSSScanner();
		} else if (extension.equals("html")) {
			return new HTMLScanner();
		} else if (extension.equals("htm")) {
			return new HTMLScanner();
		} else if (extension.equals("java")) {
			return new JavaScanner();
		} else if (extension.equals("js")) {
			return new JavaScriptScanner();
		} else if (extension.equals("rb")) {
			return new RubyScanner();
		} else if (extension.equals("ru")) {
			return new RubyScanner();
		}

		String filename = blob.getName();

		if (filename.equals("Makefile")) {
			return new MakefileScanner();
		} else if (filename.equals("Rakefile")) {
			return new RubyScanner();
		} else if (filename.equals("Gemfile")) {
			return new RubyScanner();
		}

		return null;
	}
}