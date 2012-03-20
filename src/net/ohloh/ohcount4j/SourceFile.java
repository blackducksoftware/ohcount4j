package net.ohloh.ohcount4j;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;

public class SourceFile {

	private final String path;
	private final Reader reader;
	private char[] contents = null;

	public SourceFile(String path, Reader reader) {
		this.path = path;
		this.reader = reader;
	}

	public SourceFile(String path) throws FileNotFoundException {
		this.path = path;
		this.reader = new BufferedReader(new FileReader(path));
	}

	public SourceFile(File file) throws FileNotFoundException {
		this.path = file.getPath();
		this.reader = new BufferedReader(new FileReader(file));
	}

	public SourceFile(String path, String buffer) {
		this.path = path;
		this.reader = new StringReader(buffer);
	}

	public String getPath() {
		return path;
	}

	public String getName() {
		return FilenameUtils.getName(path);
	}

	public String getExtension() {
		return FilenameUtils.getExtension(path);
	}

	public char[] getContents() throws IOException {
		// Lazy load to avoid reading until required
		if (contents == null) {
			contents = IOUtils.toCharArray(reader);
		}
		return contents;
	}

	// Regular expressions require CharSequence input
	public CharSequence getCharSequence() throws IOException {
		return java.nio.CharBuffer.wrap(getContents());
	}

	// Ideally, head() should read only as much of the file as required.
	// For now, we simply read in the entire file and return only the first portion.
	public String head(int maxLength) throws IOException {
		if (getContents().length <= maxLength) {
			return new String(getContents());
		} else {
			return new String(getContents(), 0, maxLength);
		}
	}

}
