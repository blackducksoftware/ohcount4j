package net.ohloh.ohcount4j.detect;

import net.ohloh.ohcount4j.io.Blob;
import net.ohloh.ohcount4j.scan.Scanner;

public interface Resolver {
	public Scanner resolve(Blob sourceFile);
}
