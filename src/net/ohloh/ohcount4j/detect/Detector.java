package net.ohloh.ohcount4j.detect;

import java.io.IOException;

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
		for (String binaryExtension : binaryExtensions) {
			if (binaryExtension.equalsIgnoreCase(extension)) {
				return true;
			}
		}
		return false;
	}

	private static String[] binaryExtensions = {
		"a",
		"aiff",
		"au",
		"avi",
		"bin",
		"bmp",
		"cache",
		"class",
		"dat",
		"dll",
		"doc",
		"docx",
		"dylib",
		"exe",
		"gif",
		"gz",
		"icns",
		"jar",
		"jpeg",
		"jpg",
		"m4a",
		"mov",
		"mp3",
		"mpg",
		"ogg",
		"pdf",
		"png",
		"pnt",
		"ppt",
		"pptx",
		"qt",
		"ra",
		"so",
		"svg",
		"svgz",
		"svn",
		"swf",
		"tar",
		"tgz",
		"tif",
		"tiff",
		"wav",
		"xls",
		"xlsx",
		"xlw",
		"zip"
	};

}