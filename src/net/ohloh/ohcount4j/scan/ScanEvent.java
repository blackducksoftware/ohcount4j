package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.LanguageEntity;

public class ScanEvent {
	private final Language language;
	private final LanguageEntity entity;
	private final char[] content;
	private final int position;

	public ScanEvent(Language language, LanguageEntity entity, char[] content, int position) {
		this.language = language;
		this.entity = entity;
		this.content = content;
		this.position = position;
	}

	public Language getLanguage() {
		return language;
	}

	public LanguageEntity getEntity() {
		return entity;
	}

	public char[] getContent() {
		return content;
	}

	public int getPosition() {
		return position;
	}

}
