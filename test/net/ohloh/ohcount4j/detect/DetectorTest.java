package net.ohloh.ohcount4j.detect;

import net.ohloh.ohcount4j.scan.CScanner;
import net.ohloh.ohcount4j.scan.JavaScanner;

import org.testng.annotations.Test;

public class DetectorTest {

	@Test(expectedExceptions = { IllegalArgumentException.class })
	public void errorCreateDuplicateExtensionMapping() {
		TestDetector td = new TestDetector();
		td.createDuplicateExtensionMapping();
	}

	@Test(expectedExceptions = { IllegalArgumentException.class })
	public void errorCreateDuplicateExtensionsMapping() {
		TestDetector td = new TestDetector();
		td.createDuplicateExtensionsMapping();
	}

	@Test(expectedExceptions = { IllegalArgumentException.class })
	public void errorCreateDuplicateNameMapping() {
		TestDetector td = new TestDetector();
		td.createDuplicateNameMapping();
	}

	@Test(expectedExceptions = { IllegalArgumentException.class })
	public void errorCreateDuplicateNamesMapping() {
		TestDetector td = new TestDetector();
		td.createDuplicateNamesMapping();
	}

	@Test(expectedExceptions = { IllegalArgumentException.class })
	public void errorCreateDuplicateMapResolveByExtn() {
		TestDetector td = new TestDetector();
		td.createDuplicateMapResolveByExtn();
	}

	@Test(expectedExceptions = { IllegalArgumentException.class })
	public void errorCreateDuplicateMapResolveByName() {
		TestDetector td = new TestDetector();
		td.createDuplicateMapResolveByName();
	}

	class TestDetector extends Detector {

		public TestDetector() {
			extension("java").scanUsing(JavaScanner.class);
			name("Somefile").scanUsing(JavaScanner.class);
			extension(".h").resolveUsing(ExtnHResolver.class);
			extension("Header").resolveUsing(ExtnHResolver.class);
		}

		void createDuplicateExtensionMapping() {
			extension("c").scanUsing(CScanner.class);
			extension("c").scanUsing(CScanner.class);
		}

		void createDuplicateExtensionsMapping() {
			extensions("c", "c").scanUsing(CScanner.class);
		}

		void createDuplicateNameMapping() {
			name("c").scanUsing(CScanner.class);
			name("c").scanUsing(CScanner.class);
		}

		void createDuplicateNamesMapping() {
			names("c", "c").scanUsing(CScanner.class);
		}

		void createDuplicateMapResolveByExtn() {
			extension("c").scanUsing(CScanner.class);
			extension("c").resolveUsing(ExtnHResolver.class);
		}

		void createDuplicateMapResolveByName() {
			name("c").scanUsing(CScanner.class);
			name("c").resolveUsing(ExtnHResolver.class);
		}
	}
}
