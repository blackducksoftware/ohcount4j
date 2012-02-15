package net.ohloh.ohcount4j.io;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.Reader;

import net.ohloh.ohcount4j.OhcountException;

public class FileBlob extends ReaderBlob implements Blob {

	public FileBlob(File file) throws OhcountException {
		super(file.getPath(), FileBlob.getReader(file));
	}

	private static Reader getReader(File file) throws OhcountException {
		try {
			return new BufferedReader(new FileReader(file));
		} catch (FileNotFoundException e) {
			throw new OhcountException(e.getMessage(), e);
		}
	}

}
