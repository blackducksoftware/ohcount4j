package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public interface EventHandler {

	// FIXME - remove position? it will always be 0
	public void languageStart(Language language, int position);

	public void languageEnd(Language language);

	public void entityStart(ScanEvent event);

	public void entityEnd(ScanEvent event);

	public void newline(ScanEvent event);
}
