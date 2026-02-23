.PHONY: count-lines count-lines-real count-lines-detail clean help

# Contar todas las líneas de archivos .el (excluyendo melpazoid)
count-lines:
	@echo "=== Contando líneas totales ==="
	@find . -name "*.el" -type f -not -path "./melpazoid/*" -exec wc -l {} + | tail -1

# Contar líneas de código real (sin comentarios ni líneas vacías)
count-lines-real:
	@echo "=== Contando líneas de código real (sin comentarios ni vacías) ==="
	@total=$$(find . -name "*.el" -type f -not -path "./melpazoid/*" -exec grep -v '^\s*;' {} \; | grep -v '^\s*$$' | wc -l); \
	echo "$$total líneas de código real"

# Mostrar desglose detallado por archivo
count-lines-detail:
	@echo "=== Desglose por archivo (líneas totales) ==="
	@find . -name "*.el" -type f -not -path "./melpazoid/*" -exec wc -l {} + | sort -rn
	@echo ""
	@echo "=== Desglose por archivo (código real) ==="
	@find . -name "*.el" -type f -not -path "./melpazoid/*" -print0 | \
		xargs -0 -I {} sh -c 'echo $$(grep -v "^\s*;" {} | grep -v "^\s*$$" | wc -l) {}' | \
		sort -rn

# Mostrar resumen completo
count-lines-all: count-lines count-lines-real
	@echo ""
	@total=$$(find . -name "*.el" -type f -not -path "./melpazoid/*" -exec wc -l {} + | tail -1 | awk '{print $$1}'); \
	real=$$(find . -name "*.el" -type f -not -path "./melpazoid/*" -exec grep -v '^\s*;' {} \; | grep -v '^\s*$$' | wc -l); \
	comments=$$(($$total - $$real)); \
	percentage=$$(awk "BEGIN {printf \"%.1f\", ($$real/$$total)*100}"); \
	echo "=== Resumen ==="; \
	echo "Líneas totales:    $$total"; \
	echo "Código real:       $$real ($$percentage%)"; \
	echo "Comentarios/vacías: $$comments"

# Limpiar archivos compilados
clean:
	@echo "Eliminando archivos .elc..."
	@find . -name "*.elc" -type f -not -path "./melpazoid/*" -delete
	@echo "Limpieza completada"

# Ayuda
help:
	@echo "Makefile para org-social.el"
	@echo ""
	@echo "Targets disponibles:"
	@echo "  count-lines        - Contar líneas totales"
	@echo "  count-lines-real   - Contar líneas de código real"
	@echo "  count-lines-detail - Mostrar desglose detallado"
	@echo "  count-lines-all    - Mostrar resumen completo"
	@echo "  clean              - Eliminar archivos .elc"
	@echo "  help               - Mostrar esta ayuda"
