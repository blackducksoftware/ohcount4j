package net.ohloh.ohcount4j.scan;

import static org.testng.AssertJUnit.assertEquals;
import org.testng.annotations.Test;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.Entity;
import net.ohloh.ohcount4j.scan.CScanner;

public class CScannerTest {

	@Test
	public void simple() {
		Scanner scanner = new CScanner();
		TestLineHandler h = new TestLineHandler();
		scanner.scan("#include <stdio.h>\n", h);
		assertEquals(1, h.getLines().size());
		assertEquals(Language.LANG_C, h.getLines().get(0).language);
		assertEquals(Entity.CODE, h.getLines().get(0).entity);
	}

}