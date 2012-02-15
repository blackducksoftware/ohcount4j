package net.ohloh.ohcount4j.scan;

import java.io.IOException;

import net.ohloh.ohcount4j.io.Blob;

public interface Scanner {
	public void scan(Blob blob, EventHandler handler) throws IOException;

	public void scan(char[] text, EventHandler handler);
}
