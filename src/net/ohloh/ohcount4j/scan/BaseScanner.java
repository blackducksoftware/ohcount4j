package net.ohloh.ohcount4j.scan;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.Entity;
import net.ohloh.ohcount4j.SourceFile;
import net.ohloh.ohcount4j.scan.Line;
import net.ohloh.ohcount4j.scan.LineHandler;

public abstract class BaseScanner implements Scanner {

	// Ragel variables.
	protected char[] data = null;
	protected int cs;
	protected int top;
	protected int[] stack = new int[32];
	protected int p = 0;
	protected int eof = 0;
	protected int pe = eof;
	protected int mark = 0;
	protected int ts = 0;
	protected int te = 0;
	protected int act = 0;

	ArrayList<Language> codeSeen = new ArrayList<Language>();
	ArrayList<Language> commentSeen = new ArrayList<Language>();
	protected int lineStart = 0;

	protected LineHandler handler = null;
	protected Language defaultLanguage = null;
	protected Language language = null;

	// abstract method to be implemented by scanners
	public abstract void doScan();

	protected final void init() {
		pe = eof = data.length;
	}

	public void setDefaultLanguage(Language language) {
		this.defaultLanguage = language;
	}

	public Language getDefaultLanguage() {
		return this.defaultLanguage;
	}

	@Override
	public final void scan(SourceFile source, LineHandler handler) throws IOException {
		scan(source.getContents(), handler);
	}

	@Override
	public final void scan(String data, LineHandler handler) {
		scan(data.toCharArray(), handler);
	}

	@Override
	public final void scan(char[] data, LineHandler handler) {
		this.data = data;
		this.handler = handler;
		this.language = this.defaultLanguage;
		pe = eof = data.length;
		doScan();
		if (p != lineStart) { // EOF encountered without newline
			notifyNewline();
		}
	}

	// Called by Ragel machine when moving in or out of an embedded language.
	protected void setLanguage(Language lang) {
		language = lang;
	}

	protected void notifyCode() {
		if (!codeSeen.contains(language)) {
			codeSeen.add(language);
		}
	}

	protected void notifyComment() {
		if (!commentSeen.contains(language)) {
			commentSeen.add(language);
		}
	}

	// Given lists of all the code and comment langauages we've seen on this line,
	// choose one language and entity to represent the entire line.
	protected Line chooseLine() {
		// If we've seen any language besides the default language, use that one.
		for (Language l : codeSeen) {
			if ( l != defaultLanguage ) {
				return new Line(l, Entity.CODE);
			}
		}
		for (Language l : commentSeen) {
			if ( l != defaultLanguage ) {
				return new Line(l, Entity.COMMENT);
			}
		}
		// No embedded languages. Return the default language.
		if (codeSeen.size() > 0) {
			return new Line(codeSeen.get(0), Entity.CODE);
		} else if (commentSeen.size() > 0) {
			return new Line(commentSeen.get(0), Entity.COMMENT);
		} else {
			return new Line(language, Entity.BLANK);
		}
	}

	protected void notifyNewline() {
		Line line = chooseLine().appendContent(Arrays.copyOfRange(data, lineStart, p));

		if (handler != null) {
			handler.handleLine(line);
		}

		lineStart = p;
		codeSeen.clear();
		commentSeen.clear();
	}

	protected int match_begin_mark;
	protected int match_end_mark;
	protected int match_try_mark;

	protected void beginDefineMatch() {
		match_begin_mark = p;
	}

	protected void endDefineMatch() {
		match_end_mark = p;
	}

	protected void beginTryMatch() {
		match_try_mark = p;
	}

	protected boolean match() {
		if (match_begin_mark >= match_end_mark) {
			// No back reference has been defined
			return false;
		}

		int definition_length = match_end_mark - match_begin_mark;
		int try_length = p - match_try_mark + 1;

		if (definition_length != try_length) {
			return false;
		}

		for (int i = 0; i < try_length; i++) {
			if (data[match_try_mark + i] != data[match_begin_mark + i]) {
				return false;
			}
		}

		return true;
	}
}
