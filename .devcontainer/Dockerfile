# Use the official OCaml image as the base
FROM ocaml/opam:latest

# Set the working directory
WORKDIR /workspace

# RUN apt-get update && apt-get install chktex

# Install any dependencies you might need
RUN opam install -y core ocamlformat ounit earlybird

# Copy the source code into the container
COPY . .

# Set the default command to start the OCaml REPL
CMD ["ocaml"]
