package net.ohloh.ohcount4j.scan;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.LanguageEntity;

public class LineEventHandler implements EventHandler {
	List<Line> lines = new ArrayList<Line>();
	LanguageEntity currentEntity = null;
	Line currentLine = null;
	int startOffset = 0;

	public LineEventHandler() {

	}

	@Override
	public void languageStart(Language language, int position) {
		currentLine = new Line(language);
		startOffset = position;
	}

	@Override
	public void languageEnd(Language language) {

	}

	private void processEvent(ScanEvent event) {
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
	public void entityStart(ScanEvent event) {
		processEvent(event);
		currentEntity = LanguageEntity.COMMENT;
	}

	@Override
	public void entityEnd(ScanEvent event) {
		processEvent(event);
		currentEntity = null;
	}

	@Override
	public void newline(ScanEvent event) {
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
