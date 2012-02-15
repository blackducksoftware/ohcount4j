package net.ohloh.ohcount4j;

import java.io.IOException;

import net.ohloh.ohcount4j.detect.OhcountDetector;
import net.ohloh.ohcount4j.io.Blob;
import net.ohloh.ohcount4j.scan.Scanner;

public class SourceFile {
	public SourceFile(Blob blob) throws OhcountException, IOException {
		Scanner scanner = OhcountDetector.getInstance().detect(blob);
		if (scanner != null) {
			scanner.scan(blob, null);
		}
	}
}
