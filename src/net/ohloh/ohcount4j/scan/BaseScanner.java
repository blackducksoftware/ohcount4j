package net.ohloh.ohcount4j.scan;

import java.io.IOException;
import java.util.Arrays;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.LanguageEntity;
import net.ohloh.ohcount4j.io.Blob;

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

	protected boolean codeSeen = false;
	protected boolean commentSeen = false;
	protected int lineStart = 0;

	protected EventHandler handler = null;

	protected Language language = null;

	// abstract method to be implemented by scanners
	public abstract void doScan();

	public abstract Language getLanguage();

	@Override
	public final void scan(Blob blob, EventHandler handler) throws IOException {
		data = blob.charContents();
		scan(data, handler);
	}

	protected final void init() {
		pe = eof = data.length;
	}

	@Override
	public final void scan(char[] data, EventHandler handler) {
		this.data = data;
		this.handler = handler;
		this.language = getLanguage();
		pe = eof = data.length;
		doScan();
	}

	protected void notifyCode() {
		codeSeen = true;
	}

	protected void notifyComment() {
		commentSeen = true;
	}

	protected void notifyNewline() {
		Line line = new Line(language);
		line.appendContent(Arrays.copyOfRange(data, lineStart, p));

		if (codeSeen) {
			line.setEntity(LanguageEntity.CODE);
		} else if (commentSeen) {
			line.setEntity(LanguageEntity.COMMENT);
		} else {
			line.setEntity(LanguageEntity.BLANK);
		}

		System.out.print(line.toString());

		lineStart = p;
		codeSeen = false;
		commentSeen = false;
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
