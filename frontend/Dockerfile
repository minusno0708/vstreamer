FROM node:24-slim

WORKDIR /app

RUN corepack enable && corepack prepare pnpm@latest --activate

COPY . .

EXPOSE 3000

CMD ["pnpm", "dev"]