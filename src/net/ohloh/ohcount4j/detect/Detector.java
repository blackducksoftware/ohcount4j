package net.ohloh.ohcount4j.detect;

import java.io.IOException;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.OhcountException;
import net.ohloh.ohcount4j.io.Source;
import net.ohloh.ohcount4j.scan.Scanner;

public class Detector {

	public static Scanner detect(Source source) throws OhcountException {

		if (isBinary(source.getExtension())) {
			return null;
		}

		Class<? extends Scanner> klass = null;

		try {

			if (klass == null) {
				klass = EmacsModeDetector.detect(source.head(100));
			}
			if (klass == null) {
				klass = extensionDetect(source.getExtension(), source);
			}
			if (klass == null) {
				klass = extensionDetect(source.getExtension().toLowerCase(), source);
			}
			if (klass == null) {
				klass = filenameDetect(source.getName());
			}
			if (klass == null) {
				klass = MagicDetector.detect(source.head(100));
			}

		} catch (IOException e) {
			throw new OhcountException(e);
		}

		return createInstance(klass);
	}

	private static Scanner createInstance(Class<? extends Scanner> klass) throws OhcountException {
		if (klass == null) {
			return null;
		}

		try {
			return klass.newInstance();
		} catch (InstantiationException e) {
			throw new OhcountException(e);
		} catch (IllegalAccessException e) {
			throw new OhcountException(e);
		}
	}

	private static Class<? extends Scanner> filenameDetect(String filename) {
		Language language = Language.fromFilename(filename);
		if (language != null) {
			return language.scannerClass();
		} else {
			return null;
		}
	}

	// Currently assumes extensions map uniquely to scanners.
	private static Class<? extends Scanner> extensionDetect(String ext, Source source) {
		Language language = Language.fromExtension(ext);
		if (language != null) {
			return language.scannerClass();
		} else {
			return null;
		}
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