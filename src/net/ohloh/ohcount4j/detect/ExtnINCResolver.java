package net.ohloh.ohcount4j.detect;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

public class ExtnINCResolver implements Resolver {

	@Override
	public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
		Matcher m = phpPattern.matcher(sourceFile.getCharSequence());
		if (m.find()) {
			return Language.PHP;
		} else {
			return Language.BINARY;
		}
	}

	@Override
	public Language resolve(SourceFile sourceFile) throws IOException {
		return resolve(sourceFile, new ArrayList<String>());
	}

	@Override
	public boolean canResolve(Language language) {
		if (language == Language.BINARY ||
			language == Language.PHP) {
			return true;
		} else {
			return false;
		}
	}

	private static Pattern phpPattern = Pattern.compile(
			"^\\s*<\\?php", Pattern.MULTILINE);
}