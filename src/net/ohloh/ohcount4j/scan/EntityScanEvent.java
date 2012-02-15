package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.LanguageEntity;

public class EntityScanEvent extends ScanEvent {

	private final LanguageEntity entity;

	public EntityScanEvent(Language language, LanguageEntity entity, char[] content, int position) {
		super(language, content, position);
		this.entity = entity;
	}

	public LanguageEntity getEntity() {
		return entity;
	}

}
