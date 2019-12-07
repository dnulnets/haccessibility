image-db:	
	docker build -t haccdb:1 -t haccdb:1.0 -t haccdb:latest -f deployment/Dockerfile.db .

image-svc:	
	docker build -t haccsvc:1 -t haccsvc:1.0 -t haccsvc:latest -f deployment/Dockerfile.svc .
