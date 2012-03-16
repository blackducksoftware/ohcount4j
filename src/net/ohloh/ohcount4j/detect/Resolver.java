package net.ohloh.ohcount4j.detect;

import net.ohloh.ohcount4j.io.Source;
import net.ohloh.ohcount4j.scan.Scanner;

public interface Resolver {
	public Class<? extends Scanner> resolve(Source sourceFile);
}
