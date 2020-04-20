container_name=one-fileserver
image_tag=one-fileserver
bind_port=4242

SSH_PRIVATE_KEY=`cat ~/.ssh/id_rsa`
SSH_KHS=`cat ~/.ssh/known_hosts`

registry=milesfrankel.xyz:5000
# docker login ^. usn asg0451
image_tag_with_registry=$(registry)/$(image_tag)

# debug build failures with
# docker run -it 4f84141eb59e bash

all: daemonize log-f

build: .built
# pulling and pushing schenanigans for multistage build with caching. does it work?
.built:  src/*.hs run_one.sh Dockerfile stack.yaml fileserver.cabal static/**/*
	docker pull $(image_tag_with_registry):build || true
	docker pull $(image_tag_with_registry):latest || true
	docker build -t $(image_tag_with_registry):build \
		--build-arg SSH_PRIVATE_KEY="$(SSH_PRIVATE_KEY)" \
		--build-arg SSH_KHS="$(SSH_KHS)" \
		--cache-from $(image_tag_with_registry):build \
		--target build \
		.
	docker build -t $(image_tag_with_registry):latest \
		--build-arg SSH_PRIVATE_KEY="$(SSH_PRIVATE_KEY)" \
		--build-arg SSH_KHS="$(SSH_KHS)" \
		--cache-from $(image_tag_with_registry):build \
		--cache-from $(image_tag_with_registry):run \
		--target run \
		.
	docker push $(image_tag_with_registry):build
	docker push $(image_tag_with_registry):latest
	# curl https://$(registry)/v2/one-fileserver/tags/list | jq .name -r | grep $(image_tag)
	touch .built

run: .built stop
	docker run -p $(bind_port):$(bind_port)  --name $(container_name) $(image_tag_with_registry)

daemonize: .built stop
	docker run -d -p $(bind_port):$(bind_port)  --name $(container_name) $(image_tag_with_registry)
	docker ps -a

stop:
	docker stop $(container_name) || docker kill $(container_name) || true
	docker rm $(container_name) || true

log-f: .built
	docker logs -f $(container_name)

# registry:
# 	docker run -d -p 5000:5000 --restart=always --name registry registry:2 || true
