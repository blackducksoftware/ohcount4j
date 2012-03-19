package net.ohloh.ohcount4j.detect;

import java.io.IOException;
import java.util.List;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.io.Source;

public interface Resolver {
	public Language resolve(Source sourceFile) throws IOException;
	public Language resolve(Source sourceFile, List<String> filenames) throws IOException;
	public boolean canResolve(Language language);
}
