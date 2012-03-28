package net.ohloh.ohcount4j.detect;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

public class ExtnBResolver implements Resolver {

	@Override
	public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
		if (limboPattern.matcher(sourceFile.getCharSequence()).find()) {
			return Language.LIMBO;
		} else {
			return new ExtnBASResolver().resolve(sourceFile, filenames);
		}
	}

	@Override
	public Language resolve(SourceFile sourceFile) throws IOException {
		return resolve(sourceFile, new ArrayList<String>());
	}

	@Override
	public boolean canResolve(Language language) {
		if (language == Language.CLASSIC_BASIC ||
			language == Language.STRUCTURED_BASIC ||
			language == Language.LIMBO) {
			return true;
		} else {
			return false;
		}
	}

	// /(implement[ \t])|(include[ \t]+"[^"]*";)|
    //  ((return|break|continue).*;|(pick|case).*\{)/

	private static Pattern limboPattern = Pattern.compile(
			"^\\s*implement\\s+\\S+;|" +
			"^\\s*include\\s+\"[^\"]*\"\\s*;|" +
			"\\b(return|break|continue)\\b.*;|" +
			"\\b(pick|case)\\b.*\\{"
 			);
}