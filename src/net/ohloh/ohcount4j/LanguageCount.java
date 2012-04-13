package net.ohloh.ohcount4j;

import net.ohloh.ohcount4j.scan.Line;

// Stores a total count of code, comments, etc. in a single Language
public class LanguageCount implements Comparable<LanguageCount> {
	protected Language language;
	protected int code;
	protected int comment;
	protected int blank;
	protected int fileCount;

	public LanguageCount(Language language) {
		this.language = language;
		this.code = 0;
		this.comment = 0;
		this.blank = 0;
		this.fileCount = 0;
	}

	public LanguageCount(Language language, int code, int comment, int blank) {
		this.language = language;
		this.code = code;
		this.comment = comment;
		this.blank = blank;
		this.fileCount = 0;
	}

	public LanguageCount(Line line) {
		this.language = line.getLanguage();
		this.code = 0;
		this.comment = 0;
		this.blank = 0;
		this.fileCount = 0;
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

	public float getCommentRatio() {
		if (comment == 0) {
			return 0.0f;
		} else {
			return ((float) comment / (float) (code + comment));
		}
	}

	public LanguageCount add(LanguageCount other) {
		code += other.code;
		comment += other.comment;
		blank += other.blank;
		fileCount += other.fileCount;
		return this;
	}

	public LanguageCount add(Line line) {
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

	public int incrementFileCount() {
		this.fileCount++;
		return this.fileCount;
	}

	public int getFileCount() {
		return this.fileCount;
	}

	public boolean equals(Object object) {
		if (object instanceof LanguageCount) {
			LanguageCount other = (LanguageCount) object;
			return (this.language == other.language &&
					this.code == other.code &&
					this.comment == other.comment &&
					this.blank == other.blank);
		} else {
			return false;
		}
	}

	public int compareTo(LanguageCount other) {
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