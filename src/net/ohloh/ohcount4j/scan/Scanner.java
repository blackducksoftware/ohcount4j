package net.ohloh.ohcount4j.scan;

import java.io.IOException;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

public interface Scanner {
	public void setDefaultLanguage(Language language);
	public Language getDefaultLanguage();
	public void scan(SourceFile blob, LineHandler handler) throws IOException;
	public void scan(char[] text, LineHandler handler);
	public void scan(String text, LineHandler handler);
}