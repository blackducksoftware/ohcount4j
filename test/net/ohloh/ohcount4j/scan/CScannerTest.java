package net.ohloh.ohcount4j.scan;

import java.util.List;

import static org.testng.AssertJUnit.assertEquals;
import org.testng.annotations.Test;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.LanguageEntity;
import net.ohloh.ohcount4j.scan.CScanner;
import net.ohloh.ohcount4j.scan.Line;

public class CScannerTest {

	@Test
	public void simple() {
		Scanner scanner = new CScanner();
		List<Line> lines = scanner.scan("#include <stdio.h>\n".toCharArray(), null);
		assertEquals(1, lines.size());
		assertEquals(Language.LANG_C, lines.get(0).language);
		assertEquals(LanguageEntity.CODE, lines.get(0).entity);
	}

}