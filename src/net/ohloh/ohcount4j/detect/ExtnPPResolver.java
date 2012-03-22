package net.ohloh.ohcount4j.detect;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

public class ExtnPPResolver implements Resolver {
	@Override
	public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
		// Both Pascal and Puppet have an 'include' variant, but Pascal's
		// is more strict. Thus we check for Pascal keywords first, then Puppet.
		if (pascalPattern.matcher(sourceFile.getCharSequence()).find()) {
			return Language.PASCAL;
		} else if (puppetPattern.matcher(sourceFile.getCharSequence()).find()) {
			return Language.PUPPET;
		} else {
			return Language.PASCAL;
		}
	}

	@Override
	public Language resolve(SourceFile sourceFile) throws IOException {
		return resolve(sourceFile, new ArrayList<String>());
	}

	@Override
	public boolean canResolve(Language language) {
		if (language == Language.PASCAL ||
			language == Language.PUPPET) {
			return true;
		} else {
			return false;
		}
	}

	private static Pattern pascalPattern = Pattern.compile(
			"\\bend\\.|\\{\\s*\\$i(nclude)?\\s+", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE);

	private static Pattern puppetPattern = Pattern.compile(
			"(" +
			"\\b(enable|ensure|content|source)\\s*=>|" +
			"\\binclude\\s+\\w+\\b|" +
			"\\bdefine\\s+\\w+\\s*\\(|" +
			"\\bclass\\s+\\w+\\s*\\{"+
			")", Pattern.MULTILINE);
}
