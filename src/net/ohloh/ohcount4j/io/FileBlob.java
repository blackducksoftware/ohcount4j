package net.ohloh.ohcount4j.io;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.Reader;

public class FileBlob extends ReaderBlob implements Blob {

	public FileBlob(File file) throws FileNotFoundException {
		super(file.getPath(), FileBlob.getReader(file));
	}

	private static Reader getReader(File file) throws FileNotFoundException {
		return new BufferedReader(new FileReader(file));
	}

}