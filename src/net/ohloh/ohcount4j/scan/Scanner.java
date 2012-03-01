package net.ohloh.ohcount4j.scan;

import java.io.IOException;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.io.Blob;

public interface Scanner {
	public void scan(Blob blob, LineHandler handler) throws IOException;
	public void scan(char[] text, LineHandler handler);
	public void scan(String text, LineHandler handler);
	public Language getLanguage();
}