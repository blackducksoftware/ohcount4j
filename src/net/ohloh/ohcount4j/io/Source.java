package net.ohloh.ohcount4j.io;

import java.io.IOException;
import java.io.Reader;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;

public class Source {

	private final String path;
	private final Reader reader;
	private char[] contents = null;

	public Source(String path, Reader reader) {
		this.path = path;
		this.reader = reader;
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
