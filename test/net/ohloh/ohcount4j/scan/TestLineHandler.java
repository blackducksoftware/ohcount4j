package net.ohloh.ohcount4j.scan;

import java.util.ArrayList;
import java.util.List;

import net.ohloh.ohcount4j.scan.LineHandler;

public class TestLineHandler implements LineHandler {

	List<Line> lines;

	public TestLineHandler() {
		lines = new ArrayList<Line>();
	}

	@Override
	public void handleLine(Line line) {
		lines.add(line);
	}

	public String annotation() {
		return("");
	}

	public List<Line> getLines() {
		return lines;
	}
}
