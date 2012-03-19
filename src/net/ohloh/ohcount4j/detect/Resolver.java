package net.ohloh.ohcount4j.detect;

import java.io.IOException;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.io.Source;

public interface Resolver {
	public Language resolve(Source sourceFile) throws IOException;
	public boolean canResolve(Language language);
}
