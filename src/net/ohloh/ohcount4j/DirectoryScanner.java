package net.ohloh.ohcount4j;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import net.ohloh.ohcount4j.detect.SimpleDetector;
import net.ohloh.ohcount4j.io.FileBlob;
import net.ohloh.ohcount4j.scan.LineHandler;
import net.ohloh.ohcount4j.scan.Scanner;

import org.apache.commons.io.DirectoryWalker;

public class DirectoryScanner extends DirectoryWalker<Object> {
	protected LineHandler lineHandler;

	public DirectoryScanner(LineHandler handler) {
		super();
		this.lineHandler = handler;
	}

	public void scan(File startPath) throws IOException {
		this.walk(startPath, new ArrayList<Object>());
	}

	@Override
	protected void handleFile(File file, int depth, Collection<Object> results) throws IOException {
		FileBlob blob = new FileBlob(file);
		Scanner scanner = SimpleDetector.detect(blob);
		if (scanner != null) {
			scanner.scan(blob, this.lineHandler);
		}
	}
}