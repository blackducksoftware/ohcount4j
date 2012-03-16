package net.ohloh.ohcount4j.detect;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.io.Source;

public interface Resolver {
	public Language resolve(Source sourceFile);
	public boolean canResolve(Language language);
}
