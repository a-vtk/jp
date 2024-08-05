FROM erlang:26

RUN groupadd -r jp && \
    useradd -r jp -g jp

WORKDIR /deps
COPY rebar.config rebar.lock ./
RUN rebar3 as prod compile -d

WORKDIR /src
COPY . .
RUN cp -rf /deps/_build /src/_build && \
    rebar3 as prod release && \
    cp -rf /src/_build/prod/rel/job_planner /app && \
    chown -R jp:jp /src/_build/prod/rel/job_planner

WORKDIR /app
USER jp

EXPOSE 8080

ENTRYPOINT [ "/app/bin/job_planner" ]
CMD ["foreground"]
