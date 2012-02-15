package net.ohloh.ohcount4j.scan;

import java.io.IOException;

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

	protected int mymark = 0;
	protected boolean inCode = false;

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
		handler.languageStart(getLanguage(), 0);
		doScan();
		handler.languageEnd(getLanguage());
	}

	protected void notifyNewline() {
		if (inCode) {
			notifyCodeEnd();
		}
		handler.newline(new ScanEvent(getLanguage(), LanguageEntity.NEWLINE, data, p));
	}

	protected void notifyBlanks() {
		if (inCode) {
			notifyCodeEnd();
		}
		handler.entityStart(new ScanEvent(getLanguage(), LanguageEntity.BLANK, data, ts));
		handler.entityEnd(new ScanEvent(getLanguage(), LanguageEntity.BLANK, data, te));
	}

	protected void notifyStartComment() {
		if (inCode) {
			notifyCodeEnd();
		}
		mymark = p;
		handler.entityStart(new ScanEvent(getLanguage(), LanguageEntity.COMMENT, data, mymark));
	}

	protected void notifyEndComment() {
		handler.entityEnd(new ScanEvent(getLanguage(), LanguageEntity.COMMENT, data, p));
	}

	protected void notifyStartString() {
		if (inCode) {
			notifyCodeEnd();
		}
		mymark = p;
		handler.entityStart(new ScanEvent(getLanguage(), LanguageEntity.CODE, data, mymark));
	}

	protected void notifyEndString() {
		handler.entityEnd(new ScanEvent(getLanguage(), LanguageEntity.CODE, data, p));
	}

	protected void notifyCodeCharacter() {
		if (inCode) {
			// Do nothing
		} else {
			inCode = true;
			mymark = p;
			handler.entityStart(new ScanEvent(getLanguage(), LanguageEntity.CODE, data, mymark));
		}
	}

	protected void notifyCodeEnd() {
		inCode = false;
		handler.entityEnd(new ScanEvent(getLanguage(), LanguageEntity.CODE, data, p));
	}
}
