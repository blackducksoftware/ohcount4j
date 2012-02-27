package net.ohloh.ohcount4j.scan;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import net.ohloh.ohcount4j.LanguageEntity;

public class LineEventHandler implements EventHandler {
	List<Line> lines = new ArrayList<Line>();
	LanguageEntity currentEntity = null;
	Line currentLine = null;
	int startOffset = 0;

	public LineEventHandler() {

	}

	@Override
	public void scanStart() {

	}

	@Override
	public void scanEnd(char[] data, int position) {
		if (startOffset < position) {
			currentLine.appendContent(Arrays.copyOfRange(data, startOffset, position));
		}
		// This happens when file is empty of has only blank lines.
		if (currentLine.getEntity() == null) {
			currentLine.setEntity(LanguageEntity.BLANK);
		}
		lines.add(currentLine);
	}

	@Override
	public void languageStart(ScanEvent event) {
		currentLine = new Line(event.getLanguage());
		startOffset = event.getPosition();
	}

	@Override
	public void languageEnd(ScanEvent event) {

	}

	private void processEvent(EntityScanEvent event) {
		currentLine.appendContent(Arrays.copyOfRange(event.getContent(), startOffset, event.getPosition()));
		startOffset = event.getPosition();
		if (currentLine.getEntity() == null) {
			currentLine.setEntity(event.getEntity());
			return;
		}
		switch (event.getEntity()) {
		case CODE:
			currentLine.setEntity(LanguageEntity.CODE);
			break;
		case COMMENT:
			if (currentLine.getEntity() != LanguageEntity.CODE) {
				currentLine.setEntity(LanguageEntity.COMMENT);
			}
		}
	}

	@Override
	public void entityStart(EntityScanEvent event) {
		processEvent(event);
		currentEntity = currentLine.getEntity();
	}

	@Override
	public void entityEnd(EntityScanEvent event) {
		processEvent(event);
		currentEntity = null;
	}

	@Override
	public void newline(EntityScanEvent event) {
		currentLine.appendContent(Arrays.copyOfRange(event.getContent(), startOffset, event.getPosition()));
		startOffset = event.getPosition();
		lines.add(currentLine);
		currentLine = new Line(event.getLanguage());
		currentLine.setEntity((currentEntity == null) ? LanguageEntity.BLANK : currentEntity);
	}

	public List<Line> getLines() {
		return lines;
	}

}
