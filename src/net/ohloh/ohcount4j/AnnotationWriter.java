package net.ohloh.ohcount4j;

import net.ohloh.ohcount4j.scan.Line;
import net.ohloh.ohcount4j.scan.LineHandler;

public class AnnotationWriter implements LineHandler {

	@Override
	public void handleLine(Line line) {
		System.out.print(line.toString());
	}

}
