package net.ohloh.ohcount4j.detect;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.OhcountException;

public class MagicDetector {

	// Use libmagic to identify the buffer contents.
	// Returns a Scanner class if the file type is recognized, otherwise null.
	public static Language detect(String buffer) {
		String description = getMagicDescription(buffer);
		String languageName = getLanguageName(description);

		return Detector.getInstance().detectByLanguageName(languageName);
	}

	protected static Pattern patterns[] = {
		Pattern.compile("^script text(?: executable)? for (\\w+)", 0),
		Pattern.compile("(\\w+)(?: -\\w+)* script(?:, \\w+)? text", 0),
		Pattern.compile("(\\w+) program text", 0),
	};

	public static String getLanguageName(String description) {
		if (description == null) {
			return null;
		}

		for (Pattern pattern : patterns) {
			Matcher matcher =  pattern.matcher(description);
			if (matcher.find()) {
				return matcher.group(1);
			}
		}
		return null;
	}

	public static String getMagicDescription(String buffer) {
		if (buffer == null) {
			return null;
		}

		Magic magic = new Magic();

		magic.open();
		if (magic.error() != null) {
			throw new OhcountException(magic.error());
		}

		magic.load();
		if (magic.error() != null) {
			throw new OhcountException(magic.error());
		}

		String description = magic.buffer(buffer);
		if (magic.error() != null) {
			throw new OhcountException(magic.error());
		}

		magic.close();

		return description;
	}

}