package net.ohloh.ohcount4j.detect;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.io.Source;

public class ExtnHResolver implements Resolver {

	@Override
	public Language resolve(Source sourceFile) {
		// Actual resolution is still TODO
		return Language.C;
	}

	@Override
	public boolean canResolve(Language language) {
		if (language == Language.C || language == Language.OBJECTIVE_C) {
			return true;
		}
		return false;
	}

}
