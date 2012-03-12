package net.ohloh.ohcount4j.detect;

import java.io.IOException;

import net.ohloh.ohcount4j.OhcountException;
import net.ohloh.ohcount4j.io.Source;
import net.ohloh.ohcount4j.scan.Scanner;

public class Detector {

	public static Scanner detect(Source source) throws OhcountException {

		if (ExtensionDetector.isBinary(source.getExtension())) {
			return null;
		}

		Class<? extends Scanner> klass = null;

		try {

			if (klass == null) {
				klass = EmacsModeDetector.detect(source.head(100));
			}
			if (klass == null) {
				klass = ExtensionDetector.detect(source.getExtension(), source);
			}
			if (klass == null) {
				klass = ExtensionDetector.detect(source.getExtension().toLowerCase(), source);
			}
			if (klass == null) {
				klass = FilenameDetector.detect(source.getName());
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
}