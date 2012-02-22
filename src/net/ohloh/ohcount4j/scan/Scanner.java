package net.ohloh.ohcount4j.scan;

import java.io.IOException;
import java.util.List;

import net.ohloh.ohcount4j.io.Blob;
import net.ohloh.ohcount4j.scan.Line;

public interface Scanner {
	public List<Line> scan(Blob blob, EventHandler handler) throws IOException;
	public List<Line> scan(char[] text, EventHandler handler);
}