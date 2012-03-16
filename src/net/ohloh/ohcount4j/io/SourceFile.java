package net.ohloh.ohcount4j.io;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;

public class SourceFile extends Source {

	public SourceFile(String path) throws FileNotFoundException {
		super(path, new BufferedReader(new FileReader(path)));
	}

	public SourceFile(File file) throws FileNotFoundException {
		super(file.getPath(), new BufferedReader(new FileReader(file)));
	}

}