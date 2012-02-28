package net.ohloh.ohcount4j;

import net.ohloh.ohcount4j.scan.Line;

public class Count implements Comparable<Count> {
	protected Language language;
	protected int code;
	protected int comment;
	protected int blank;

	public Count(Language language) {
		this.language = language;
		this.code = 0;
		this.comment = 0;
		this.blank = 0;
	}

	public Count(Language language, int code, int comment, int blank) {
		this.language = language;
		this.code = code;
		this.comment = comment;
		this.blank = blank;
	}

	public Count(Line line) {
		this.language = line.getLanguage();
		this.code = 0;
		this.comment = 0;
		this.blank = 0;
		add(line);
	}

	public Language getLanguage() {
		return language;
	}

	public int getCode() {
		return code;
	}

	public int getComment() {
		return comment;
	}

	public int getBlank() {
		return blank;
	}

	public int getTotal() {
		return code + comment + blank;
	}

	public Count add(Count other) {
		code += other.code;
		comment += other.comment;
		blank += other.blank;
		return this;
	}

	public Count add(Line line) {
		switch(line.getEntity()) {
		case CODE:
			code += 1;
			break;
		case COMMENT:
			comment += 1;
			break;
		case BLANK:
			blank += 1;
			break;
		}
		return this;
	}

	public boolean equals(Object object) {
		if (object instanceof Count) {
			Count other = (Count) object;
			return (this.language == other.language &&
					this.code == other.code &&
					this.comment == other.comment &&
					this.blank == other.blank);
		} else {
			return false;
		}
	}

	public int compareTo(Count other) {
		if (this.getCode() == other.getCode()) {
			if (this.getComment() == other.getComment()) {
				return new Integer(this.getBlank()).compareTo(other.getBlank());
			} else {
				return new Integer(this.getComment()).compareTo(other.getComment());
			}
		} else {
			return new Integer (this.getCode()).compareTo(other.getCode());
		}
	}
}