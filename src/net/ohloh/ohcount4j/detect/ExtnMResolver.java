package net.ohloh.ohcount4j.detect;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

public class ExtnMResolver implements Resolver {

	@Override
	public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
		Language result = null;
		return result;
	}

	@Override
	public Language resolve(SourceFile sourceFile) throws IOException {
		return resolve(sourceFile, new ArrayList<String>());
	}

	@Override
	public boolean canResolve(Language language) {
		if (language == Language.LIMBO ||
			language == Language.MATLAB ||
			language == Language.OCTAVE ||
			language == Language.OBJECTIVE_C) {
			return true;
		}
		return false;
	}

}