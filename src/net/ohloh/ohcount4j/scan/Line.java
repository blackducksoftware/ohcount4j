package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.LanguageEntity;

public class Line {
	Language language;
	LanguageEntity entity;
	StringBuilder content;

	public Line() {
		content = new StringBuilder();
	}

	public Line(Language language) {
		this.language = language;
		content = new StringBuilder();
	}

	@Override
	public String toString() {
		return String.format("%1$s\t%2$s\t%3$s", language.uname(), entity.name(), content);
	}

	public Language getLanguage() {
		return language;
	}

	public Line setLanguage(Language language) {
		this.language = language;
		return this;
	}

	public LanguageEntity getEntity() {
		return entity;
	}

	public Line setEntity(LanguageEntity entity) {
		this.entity = entity;
		return this;
	}

	public Line appendContent(char[] content) {
		this.content.append(content);
		return this;
	}

	public String getContent() {
		return content.toString();
	}

	public Line setContent(String content) {
		this.content = new StringBuilder(content);
		return this;
	}
}
