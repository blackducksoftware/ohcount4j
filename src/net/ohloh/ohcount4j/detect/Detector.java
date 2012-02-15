package net.ohloh.ohcount4j.detect;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.ohloh.ohcount4j.OhcountException;
import net.ohloh.ohcount4j.io.Blob;
import net.ohloh.ohcount4j.scan.Scanner;

public abstract class Detector {
	protected Map<String, Mapping> extensionMappings = new HashMap<String, Mapping>();
	protected Map<String, Mapping> nameMappings = new HashMap<String, Mapping>();

	public Scanner detect(Blob sourceFile) throws OhcountException {
		Mapping mapping = nameMappings.get(sourceFile.getName());
		if (mapping == null) {
			mapping = extensionMappings.get(sourceFile.getExtension());
		}
		if (mapping == null) {
			return null;
		}
		if (mapping instanceof ScannerMapping) {
			return createInstance(((ScannerMapping) mapping).scannerClass);
		} else {
			return createInstance(((ResolverMapping) mapping).resolverClass).resolve(sourceFile);
		}
	}

	protected MappingConfig extension(String extn) {
		return new MappingConfig(Mapping.Type.EXTN, extn);
	}

	protected MappingConfig extensions(String... extns) {
		return new MappingConfig(Mapping.Type.EXTN, extns);
	}

	protected MappingConfig name(String name) {
		return new MappingConfig(Mapping.Type.NAME, name);
	}

	protected MappingConfig names(String... names) {
		return new MappingConfig(Mapping.Type.NAME, names);
	}

	private <T> T createInstance(Class<T> klass) throws OhcountException {
		try {
			return klass.newInstance();
		} catch (InstantiationException e) {
			throw new OhcountException(e);
		} catch (IllegalAccessException e) {
			throw new OhcountException(e);
		}
	}

	private static class Mapping {
		private enum Type {
			NAME, EXTN;
		}

		final String key;
		final Type type;

		private Mapping(String key, Type type) {
			this.key = key;
			this.type = type;
		}

		private Mapping(Mapping other) {
			this.key = other.key;
			this.type = other.type;
		}
	}

	private static class ScannerMapping extends Mapping {
		final Class<? extends Scanner> scannerClass;

		private ScannerMapping(Mapping other, Class<? extends Scanner> scannerClass) {
			super(other);
			this.scannerClass = scannerClass;
		}
	}

	private static class ResolverMapping extends Mapping {
		final Class<? extends Resolver> resolverClass;

		private ResolverMapping(Mapping other, Class<? extends Resolver> resolverClass) {
			super(other);
			this.resolverClass = resolverClass;
		}
	}

	public class MappingConfig {

		final List<Mapping> mappings;

		private MappingConfig(Mapping.Type type, String... keys) {
			mappings = new ArrayList<Mapping>();
			for (String k : keys) {
				mappings.add(new Mapping(k, type));
			}
		}

		public void scanUsing(Class<? extends Scanner> scannerClass) {
			for (Mapping m : mappings) {
				addToMap((m.type == Mapping.Type.EXTN ? extensionMappings : nameMappings), new ScannerMapping(m,
						scannerClass));
			}
		}

		public void resolveUsing(Class<? extends Resolver> resolverClass) {
			for (Mapping m : mappings) {
				addToMap((m.type == Mapping.Type.EXTN ? extensionMappings : nameMappings), new ResolverMapping(m,
						resolverClass));
			}
		}

		private void addToMap(Map<String, Mapping> map, Mapping m) {
			if (map.containsKey(m.key)) {
				throw new IllegalArgumentException("Cannot create duplicate mapping for " + m.key);
			}
			map.put(m.key, m);
		}
	}

}
