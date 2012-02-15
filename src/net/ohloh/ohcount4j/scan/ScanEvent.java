package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class ScanEvent {
	private final Language language;
	private final char[] content;
	private final int position;

	public ScanEvent(Language language, char[] content, int position) {
		this.language = language;
		this.content = content;
		this.position = position;
	}

	public Language getLanguage() {
		return language;
	}

	public char[] getContent() {
		return content;
	}

	public int getPosition() {
		return position;
	}

}
