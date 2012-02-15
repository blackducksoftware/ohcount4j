package net.ohloh.ohcount4j.scan;

public interface EventHandler {

	public void scanStart();

	public void scanEnd(char[] data, int position);

	// FIXME - remove position? it will always be 0
	public void languageStart(ScanEvent event);

	public void languageEnd(ScanEvent event);

	public void entityStart(EntityScanEvent event);

	public void entityEnd(EntityScanEvent event);

	public void newline(EntityScanEvent event);
}
