package net.ohloh.ohcount4j.io;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;

public class InputStreamBlob extends ReaderBlob implements Blob {

	public InputStreamBlob(String path, InputStream in) {
		super(path, new BufferedReader(new InputStreamReader(in)));
	}

}
