package net.ohloh.ohcount4j;

import net.ohloh.ohcount4j.Count;
import net.ohloh.ohcount4j.CountList;
import net.ohloh.ohcount4j.scan.Line;
import net.ohloh.ohcount4j.scan.LineHandler;

public class SummaryWriter implements LineHandler {
	protected CountList countList;

	public SummaryWriter() {
		this.countList = new CountList();
	}

	@Override
	public void handleLine(Line line) {
		countList.add(line);
	}
}
