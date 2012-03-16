package net.ohloh.ohcount4j.io;

import java.io.StringReader;

public class SourceBuffer extends Source {

	public SourceBuffer(String path, String buffer) {
		super(path, new StringReader(buffer));
	}

}
