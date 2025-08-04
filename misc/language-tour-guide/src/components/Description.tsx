interface DescriptionProps {
  children: React.ReactNode;
}

function Description({ children }: DescriptionProps) {
  return (
    <section className="lg:row-span-2 bg-light-30 p-4 lg:p-8 wrap-break-word lg:overflow-y-auto h-full border border-grey-20 scrollbar">
      {children}
    </section>
  );
}

export default Description;
