package net.ohloh.ohcount4j.detect;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.OhcountException;
import net.ohloh.ohcount4j.io.Source;

public class Detector {

	public static Language detect(Source source) throws OhcountException {

		if (isBinary(source.getExtension())) {
			return null;
		}

		Language language = null;

		try {

			if (language == null) {
				language = EmacsModeDetector.detect(source.head(100));
			}
			if (language == null) {
				language = extensionDetect(source.getExtension(), source);
			}
			if (language == null) {
				language = extensionDetect(source.getExtension().toLowerCase(), source);
			}
			if (language == null) {
				language = Language.fromFilename(source.getName());
			}
			if (language == null) {
				language = MagicDetector.detect(source.head(100));
			}

		} catch (IOException e) {
			throw new OhcountException(e);
		}

		return language;
	}

	// Currently assumes extensions map uniquely to scanners.
	private static Language extensionDetect(String ext, Source source) {
		return Language.fromExtension(ext);
	}

	public static boolean isBinary(String extension) {
		return binaryExtensions.contains(extension.toLowerCase());
	}

	@SuppressWarnings("serial")
	private static Set<String> binaryExtensions = new HashSet<String>() {{
		add("a");
		add("aiff");
		add("au");
		add("avi");
		add("bin");
		add("bmp");
		add("cache");
		add("class");
		add("dat");
		add("dll");
		add("doc");
		add("docx");
		add("dylib");
		add("exe");
		add("gif");
		add("gz");
		add("ico");
		add("icns");
		add("jar");
		add("jpeg");
		add("jpg");
		add("m4a");
		add("mov");
		add("mp3");
		add("mpg");
		add("ogg");
		add("pdf");
		add("png");
		add("pnt");
		add("ppt");
		add("pptx");
		add("qt");
		add("ra");
		add("so");
		add("svg");
		add("svgz");
		add("svn");
		add("swf");
		add("tar");
		add("tgz");
		add("tif");
		add("tiff");
		add("wav");
		add("xls");
		add("xlsx");
		add("xlw");
		add("zip");
	}};

}