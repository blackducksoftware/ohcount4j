package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.Entity;

public class Line {
	Language language;
	Entity entity;
	StringBuilder content;

	public Line() {
		content = new StringBuilder();
	}

	public Line(Language language) {
		this.language = language;
		this.content = new StringBuilder();
	}

	public Line(Language language, Entity entity) {
		this.language = language;
		this.entity = entity;
		this.content = new StringBuilder();
	}

	public Line(Language language, Entity entity, String content) {
		this.language = language;
		this.entity = entity;
		this.content = new StringBuilder(content);
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

	public Entity getEntity() {
		return entity;
	}

	public Line setEntity(Entity entity) {
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
